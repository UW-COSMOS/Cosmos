"""Add pp_rule_cls for rule-based postprocessing outcomes

Revision ID: 4e23c74afe9c
Revises: 2ff279fd603c
Create Date: 2020-03-20 19:02:13.921276

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = '4e23c74afe9c'
down_revision = '2ff279fd603c'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('page_objects', Column('pp_rule_cls', String(200)))


def downgrade():
    op.drop_column('page_objects', 'pp_rule_cls')

