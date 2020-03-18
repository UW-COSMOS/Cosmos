"""Add ObjectContexts

Revision ID: de8dcadbd2cb
Revises: 
Create Date: 2020-03-16 21:00:05.356877

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = 'de8dcadbd2cb'
down_revision = None
branch_labels = None
depends_on = None


def upgrade():
    op.create_table(
        'object_contexts',
        Column('id', Integer, primary_key=True),
        Column('pdf_id', Integer, ForeignKey('pdfs.id')),
        Column('header_id', Integer, ForeignKey('page_objects.id')),
        Column('header_content', Text()),
        Column('content', Text())
    )

    op.add_column('page_objects', 
        Column('context_id', Integer, ForeignKey('object_contexts.id')))

