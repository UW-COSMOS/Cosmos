"""update summary text length

Revision ID: 7ca1f041894f
Revises: 031c2e1e6db3
Create Date: 2020-05-12 18:12:37.515736

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '7ca1f041894f'
down_revision = '031c2e1e6db3'
branch_labels = None
depends_on = None


def upgrade():
    op.alter_column('object_contexts', 'summary', type_=sa.Text(2**32-1), existing_nullable=True, existing_comment='', existing_autoincrement=False, server_default=None)

def downgrade():
    pass
